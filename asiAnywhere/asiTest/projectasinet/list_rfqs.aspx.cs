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
public partial class list_rfqs : System.Web.UI.Page
{
    public list_rfqs()
    {
        //
        // TODO: Add constructor logic here
        //
    }
        

    protected void Page_Load(object sender, EventArgs e)
    {

        Session["my_new_rfq"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;       
      
        if (!Page.IsPostBack)
        {
            txt_l.Text = "0.0000";
            txt_w.Text = "0.0000";
            txt_d.Text = "0.0000";

            if (Session["RfqNo"] != null)
                txt_rfq.Text = Convert.ToString(Session["RfqNo"]);
            else
                txt_rfq.Text = "";
            if (Session["my_new_customer_text"] != null)
                txt_customer.Text = Convert.ToString(Session["my_new_customer_text"]);
            else
                txt_customer.Text = "";
            if (Session["rfqPartno"] != null)
                txt_custpart.Text = Convert.ToString(Session["rfqPartno"]);
            else
                txt_custpart.Text = "";
            if (Session["rfqPartDscr"] != null)
                txt_partdescription.Text = Convert.ToString(Session["rfqPartDscr"]);
            else
                txt_partdescription.Text = "";
            if (Session["rfqStyle"] != null)
                txt_style.Text = Convert.ToString(Session["rfqStyle"]);
            else
                txt_style.Text = "";
            if (Session["rfqEst"] != null)
                txt_estimate.Text = Convert.ToString(Session["rfqEst"]);
            else
                txt_estimate.Text = "";
            if (Session["rfqLength"] != null)
                txt_l.Text = Convert.ToString(Session["rfqLength"]);

            if (Session["rfqWidth"] != null)
                txt_w.Text = Convert.ToString(Session["rfqWidth"]);

            if (Session["rfqDepth"] != null)
                txt_d.Text = Convert.ToString(Session["rfqDepth"]);
        }

        Session["prmAction"] = Convert.ToString(Session["prmAction"]);
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        String expression = "vRfqNo";
        SortDirection direction;
        direction = SortDirection.Descending;
        GridView1.Sort(expression, direction);
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "List Rfqs";
        //ImageButton brwsorder = (ImageButton)Master.FindControl("list_rfqs");
        //brwsorder.ImageUrl = "Images/list RFQS1.jpg";
              
        GridView1.PageIndex = Convert.ToInt32(Session["page_inder"]);

        GridView1.SelectedIndex = Convert.ToInt32(Session["list_rfq_grid_seqno_index"]) - 1;
        try
        {
            if (Session["list_rfq_grid_seqno_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["rfqsnos"] = GridView1.SelectedRow.Cells[1].Text;
                Session["list_rfq_old_est"] = GridView1.SelectedRow.Cells[16].Text;

                if (Convert.ToString(Session["list_rfq_old_est"]) != "" && Convert.ToString(Session["list_rfq_old_est"]) != "&nbsp;")
                {
                    Session["my_new_est_no"] = "1";
                }
                else
                {
                    Session["my_new_est_no"] = null;
                }



                foreach (GridViewRow gv in GridView1.Rows)
                {
                    Session["list_rfq_grid_seqno"] = ((Label)GridView1.SelectedRow.FindControl("Label_seqno")).Text;
                    Session["list_rfq_rfq_nos"] = GridView1.SelectedRow.Cells[1].Text;
                    Session["list_rfq_cust_part_no"] = GridView1.SelectedRow.Cells[5].Text;
                    Session["list_rfq_cust_no"] = GridView1.SelectedRow.Cells[4].Text;


                }
                //Session["rfqcustpart"] = Session["list_rfq_grid_seqno"];
                //Session["rfqcust"] = Session["list_rfq_cust_no"];
            }
        }
        catch
        {
            return;
        }

        txt_rfq.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_customer.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_custpart.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_partdescription.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_style.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_estimate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_l.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_w.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_d.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");

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
            string vPage = "list_rfqs.aspx";
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
        ObjectDataSource1.SelectParameters["vRfqNo"].DefaultValue = txt_rfq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_customer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPartno"].DefaultValue = txt_custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["vPartDscr"].DefaultValue = txt_partdescription.Text.Trim();
        ObjectDataSource1.SelectParameters["vStyle"].DefaultValue = txt_style.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = txt_estimate.Text.Trim();
        ObjectDataSource1.SelectParameters["vLength"].DefaultValue = txt_l.Text.Trim();
        ObjectDataSource1.SelectParameters["vWidth"].DefaultValue = txt_w.Text.Trim();
        ObjectDataSource1.SelectParameters["vDepth"].DefaultValue = txt_d.Text.Trim();


        Session["prmAction"] = "Search";
        Session["RfqNo"] = txt_rfq.Text.Trim();
        Session["rfqCust"] = txt_customer.Text.Trim();
        Session["rfqPartno"] = txt_custpart.Text.Trim();
        Session["rfqPartDscr"] = txt_partdescription.Text.Trim();
        Session["rfqStyle"] = txt_style.Text.Trim();
        Session["rfqEst"] = txt_estimate.Text.Trim();
        Session["rfqLength"] = txt_l.Text.Trim();
        Session["rfqWidth"] = txt_w.Text.Trim();
        Session["rfqDepth"] = txt_d.Text.Trim();
        Session["list_rfq_grid_seqno_index"] = null;
        Session["list_rfq_grid_seqno"] = null;
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
        txt_customer.Text = "";
        Session["gridsize"] = null;
        GridView1.SelectedIndex = 0;
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = txt_customer.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_customer.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["vRfqNo"].DefaultValue = txt_rfq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_customer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPartno"].DefaultValue = txt_custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["vPartDscr"].DefaultValue = txt_partdescription.Text.Trim();
        ObjectDataSource1.SelectParameters["vStyle"].DefaultValue = txt_style.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = txt_estimate.Text.Trim();
        ObjectDataSource1.SelectParameters["vLength"].DefaultValue = txt_l.Text.Trim();
        ObjectDataSource1.SelectParameters["vWidth"].DefaultValue = txt_w.Text.Trim();
        ObjectDataSource1.SelectParameters["vDepth"].DefaultValue = txt_d.Text.Trim();


        Session["prmAction"] = "Select";
        Session["RfqNo"] = null;
        Session["rfqCust"] = null;
        Session["rfqPartno"] = null;
        Session["rfqPartDscr"] = null;
        Session["rfqStyle"] = null;
        Session["rfqEst"] = null;
        Session["rfqLength"] = null;
        Session["rfqWidth"] = null;
        Session["rfqDepth"] = null;
        Session["list_rfq_grid_seqno_index"] = null;
        Session["list_rfq_grid_seqno"] = null;
        txt_l.Text = "0.0000";
        txt_w.Text = "0.0000";
        txt_d.Text = "0.0000";
        Session["my_new_customer_text"] = null;
        Session["page_inder"] = 0;

        GridView1.PageIndex = Convert.ToInt32(Session["page_inder"]);
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {        
        Session["rfqsnos"] = GridView1.SelectedRow.Cells[1].Text;
        Session["list_rfq_old_est"] = GridView1.SelectedRow.Cells[16].Text;    

        if (Convert.ToString(Session["list_rfq_old_est"]) != "" && Convert.ToString(Session["list_rfq_old_est"]) != "&nbsp;")
        {
            Session["my_new_est_no"] = "1";
        }
        else
        {
            Session["my_new_est_no"] = null;
        }


        Session["list_rfq_grid_seqno_index"] = GridView1.SelectedIndex + 1;
        foreach (GridViewRow gv in GridView1.Rows)
        {
            Session["list_rfq_grid_seqno"] = ((Label)GridView1.SelectedRow.FindControl("Label_seqno")).Text;
            Session["list_rfq_rfq_nos"] = GridView1.SelectedRow.Cells[1].Text;
            Session["list_rfq_cust_part_no"] = GridView1.SelectedRow.Cells[5].Text;
            Session["list_rfq_cust_no"] = GridView1.SelectedRow.Cells[4].Text;
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
        Session["page_inder"] = e.NewPageIndex;      
    }
    protected void customerid_TextChanged(object sender, EventArgs e)
    {
        Session["my_new_customer_text"] = txt_customer.Text;
    }
}
