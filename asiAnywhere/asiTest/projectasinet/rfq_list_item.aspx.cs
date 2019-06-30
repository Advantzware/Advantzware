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
using System.Text;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfq_list_item : System.Web.UI.Page
{
    
    public rfq_list_item()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        //Response.Write(Session["list_rfq_cust_part_no"]);

        Session["my_new_rfq"] = null;
        

        UserClass.CheckLogin(Page);

        //ImageButton brwsorder = (ImageButton)Master.FindControl("list_item");
        //brwsorder.ImageUrl = "~/Images/lisl item 1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Rfq Item";
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        

        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;


        // FormView1.ChangeMode(FormViewMode.ReadOnly);
        Session["total_rfq_item_grid_rows"] = GridView1.Rows.Count;
        GridView1.SelectedIndex = Convert.ToInt32(Session["list_rfq_grid_seqno"]) - 1;

        try
        {
            Session["list_rfq_grid_row_count"] = GridView1.Rows.Count;
            if (Session["list_rfq_grid_seqno"] != null)
            {
                
                Session["rfqcustpart"] = GridView1.SelectedRow.Cells[1].Text;             
                Session["list_rfq_cust_part_no"] = GridView1.SelectedRow.Cells[5].Text;
                Session["list_rfq_cust_style"] =((Label)GridView1.SelectedRow.FindControl("Label1")).Text;                
            }
        }
        catch
        {
            return;
        }
        //GridView1.SelectedIndex = Convert.ToInt32(Session["rfqgridindex"]);

        //Response.Write(Session["list_rfq_grid_row_count"]);
        if(Convert.ToInt32(Session["list_rfq_grid_row_count"])>1)
        {
            Session["list_rfq_cust_style"]=null;
        }
        //Response.Write(Session["list_rfq_cust_style"]);
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                ///UserClass UserLogin = (UserClass)Session["User"];

                string vUserId = UserLogin.UserName;
                string vPage = "rfqitem.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                if (vCanRun == true)
                {
                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }
        }


    }

    
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["list_rfq_grid_seqno"] = GridView1.SelectedIndex + 1;
        Session["rfqcustpart"] = GridView1.SelectedRow.Cells[1].Text;
        Session["list_rfq_cust_part_no"] = GridView1.SelectedRow.Cells[5].Text;
        Session["list_rfq_cust_style"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
        //foreach (GridViewRow gv in GridView1.Rows)
        //{
        //    Session["rfqsequenceno"] = ((Label)GridView1.SelectedRow.FindControl("rfqrowid")).Text;
        //    //Response.Write(Session["rfqsequenceno"]);
        //}
    }

    protected void rfq_estimate(object sender, EventArgs e)
    {

        Session["rfqestno"] = Session["rfqsnos"];
        Response.Redirect("rfq_estimate.aspx");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "rfqitem.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;
            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
        }
    }

    
}
