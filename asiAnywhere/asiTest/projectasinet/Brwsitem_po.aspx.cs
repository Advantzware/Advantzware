using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class Brwsitem_po : System.Web.UI.Page
{
    public Brwsitem_po()
    {
        //
        // TODO: Add constructor logic here
        //
    }
        

    protected void Page_Load(object sender, EventArgs e)
    {

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;
   
        if (!Page.IsPostBack)
        {
                      
            
        }
                
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;


        //ImageButton brwsorder = (ImageButton)Master.FindControl("list_item");
        //brwsorder.ImageUrl = "Images/lisl item 1.jpg";
        Image movebutton = (Image)Master.FindControl("Image5");
        movebutton.Visible = false;
              
        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_po_item"]);

        GridView1.SelectedIndex = Convert.ToInt32(Session["list_po_grid_seqno_item_index"]) - 1;
        try
        {
            if (Session["list_po_grid_seqno_item_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["pur_ord_po_line_no"] = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
                Session["item"] = GridView1.SelectedRow.Cells[1].Text;
            }
        }
        catch
        {
            return;
        }

        txt_po.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");        
        txt_ino.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");        

        try
        {
            TextBox vsearch = (TextBox)FormView1.FindControl("aLineLabel");
            vsearch.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch
        { }


        if (Session["gridsize"] != null)
        {
            //GridView1.PageSize = Convert.ToInt32(Session["gridsize"]);
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }




        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "Brwslist_po.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }

        }

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmpoNo"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoItemNo"].DefaultValue = txt_ino.Text.Trim();
                            
        Session["list_po_grid_seqno_item_index"] = null;
        GridView1.SelectedIndex = 0;

    }

    protected void btn_reset_Click(object sender, EventArgs e)
    { 
        ContentPlaceHolder ct = (ContentPlaceHolder)Master.FindControl("ContentPlaceHolder1");
        foreach (Control c in ct.Controls)
        {
            switch (c.GetType().ToString())
            {
                case "System.Web.UI.WebControls.TextBox":
                    ((TextBox)c).Text = "";
                    break;
            }
        }
        
        Session["gridsize"] = null;
        GridView1.SelectedIndex = 0;
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmpoNo"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoItemNo"].DefaultValue = txt_ino.Text.Trim();
                      
        
        Session["list_po_grid_seqno_item_index"] = null;       
        
        Session["page_inder_po_item"] = 0;

        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_po_item"]);
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["pur_ord_po_line_no"] = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
        Session["item"] = GridView1.SelectedRow.Cells[1].Text;
        Session["list_po_grid_seqno_item_index"] = GridView1.SelectedIndex + 1;
       
    }

    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {

        GridView1.PageIndex = e.NewPageIndex;
        Session["page_inder_po_item"] = e.NewPageIndex;      
    }
   
    
}
