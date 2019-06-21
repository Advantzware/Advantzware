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
public partial class list_set_part : System.Web.UI.Page
{
    public list_set_part()
    {
        //
        // TODO: Add constructor logic here
        //
    }
        

    protected void Page_Load(object sender, EventArgs e)
    {

        Session["my_new_rcpt"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;       
      
        if (!Page.IsPostBack)
        {

            if (Session["prmSeq_rcpt2"] != null)
                txt_seq.Text = Convert.ToString(Session["prmSeq_rcpt2"]);
            else
                txt_seq.Text = "";
            if (Session["prmTag_rcpt2"] != null)
                txt_tag.Text = Convert.ToString(Session["prmTag_rcpt2"]);
            else
                txt_tag.Text = "";
            if (Session["prmDate_rcpt2"] != null)
                txt_date.Text = Convert.ToString(Session["prmDate_rcpt2"]);
            else
                txt_date.Text = "";
            if (Session["prmPo_rcpt2"] != null)
                txt_po.Text = Convert.ToString(Session["prmPo_rcpt2"]);
            else
                txt_po.Text = "";
            if (Session["prmIno_rcpt2"] != null)
                txt_ino.Text = Convert.ToString(Session["prmIno_rcpt2"]);
            else
                txt_ino.Text = "";
            if (Session["prmJob_rcpt2"] != null)
                txt_job.Text = Convert.ToString(Session["prmJob_rcpt2"]);
            else
                txt_job.Text = "";
            
        }

        Session["prmAction"] = Convert.ToString(Session["prmAction"]);
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;


        //ImageButton brwsorder = (ImageButton)Master.FindControl("list_item");
        //brwsorder.ImageUrl = "Images/set-parts.jpg";
              
        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_rcpt"]);

        GridView1.SelectedIndex = Convert.ToInt32(Session["list_rcpt_grid2_seqno_index"]) - 1;
        try
        {
            if (Session["list_rcpt_grid2_seqno_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                //Session["seqno"] = GridView1.SelectedRow.Cells[1].Text;
            }
        }
        catch
        {
            return;
        }

        txt_seq.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_tag.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_date.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_po.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_ino.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        

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
            string vPage = "list_rcpt.aspx";
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

            /*if (aUsers == "external")
            {
                customerid.Visible = false;

            }
            */

        }

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = txt_seq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_ino.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

        Session["prmAction_rcpt2"] = "Search";        
        Session["prmSeq_rcpt2"] = null;
        Session["prmTag_rcpt2"] = null;
        Session["prmDate_rcpt2"] = null;
        Session["prmPo_rcpt2"] = null;
        Session["prmIno_rcpt2"] = null;
        Session["prmJob_rcpt2"] = null;
        
        Session["list_rcpt_grid2_seqno_index"] = null;
        Session["list_rcpt_grid2_seqno"] = null;
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
        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = txt_customer.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_customer.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = txt_seq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_ino.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

        Session["prmAction_rcpt2"] = "Select";
        Session["prmSeq_rcpt2"] = null;
        Session["prmTag_rcpt2"] = null;
        Session["prmDate_rcpt2"] = null;
        Session["prmPo_rcpt2"] = null;
        Session["prmIno_rcpt2"] = null;
        Session["prmJob_rcpt2"] = null;
        
        Session["list_rcpt_grid2_seqno_index"] = null;
        Session["list_rcpt_grid2_seqno"] = null;       
        
        Session["page_inder_rcpt"] = 0;

        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_rcpt"]);
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {       
              
        Session["list_rcpt_grid2_seqno_index"] = GridView1.SelectedIndex + 1;
        foreach (GridViewRow gv in GridView1.Rows)
        {
            //Session["list_rcpt_grid2_seqno"] = ((Label)GridView1.SelectedRow.FindControl("Label_seqno")).Text;
            //Session["list_rfq_rfq_nos"] = GridView1.SelectedRow.Cells[1].Text;
            //Session["list_rfq_cust_part_no"] = GridView1.SelectedRow.Cells[5].Text;
            //Session["list_rfq_cust_no"] = GridView1.SelectedRow.Cells[4].Text;
        }        
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
        Session["page_inder_rcpt"] = e.NewPageIndex;      
    }
   /* protected void customerid_TextChanged(object sender, EventArgs e)
    {
        Session["my_new_customer_text"] = txt_customer.Text;
    }*/
}
