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
using ASINET;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>

namespace projectasinet.sharpshooter
{
    public partial class delete_list_rcptss : System.Web.UI.Page
    {
        public delete_list_rcptss()
        {
            //
            // TODO: Add constructor logic here
            //
        }


        protected void Page_Load(object sender, EventArgs e)
        {
            Session["master_rece_delete_rece_list"] = "delete";
            Session["my_new_rcpt"] = null;
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Session["Rowuser"] = UserLogin.UserName;
            Session["prmUser"] = UserLogin.UserName;

            if (!Page.IsPostBack)
            {

                if (Session["prmSeq_rcpt_gd"] != null)
                    txt_seq.Text = Convert.ToString(Session["prmSeq_rcpt_gd"]);
                else
                    txt_seq.Text = "";
                if (Session["prmTag_rcpt_gd"] != null)
                    txt_tag.Text = Convert.ToString(Session["prmTag_rcpt_gd"]);
                else
                    txt_tag.Text = "";
                if (Session["prmDate_rcpt_gd"] != null)
                    txt_date.Text = Convert.ToString(Session["prmDate_rcpt_gd"]);
                else
                    txt_date.Text = "";
                if (Session["prmPo_rcpt_gd"] != null)
                    txt_po.Text = Convert.ToString(Session["prmPo_rcpt_gd"]);
                else
                    txt_po.Text = "";
                if (Session["prmIno_rcpt_gd"] != null)
                    txt_ino.Text = Convert.ToString(Session["prmIno_rcpt_gd"]);
                else
                    txt_ino.Text = "";
                if (Session["prmJob_rcpt_gd"] != null)
                    txt_job.Text = Convert.ToString(Session["prmJob_rcpt_gd"]);
                else
                    txt_job.Text = "";

            }

            Session["prmAction"] = Convert.ToString(Session["prmAction"]);
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            Label heading = (Label)Master.FindControl("Label_heading");
            heading.Text = "Delete Finished Goods :";

            ImageButton brwsorder = (ImageButton)Master.FindControl("recelist_rcpt");
            brwsorder.ImageUrl = "~/Images/list-receipts1.jpg";

            GridView1.PageIndex = Convert.ToInt32(Session["page_inder_gd"]);

            GridView1.SelectedIndex = Convert.ToInt32(Session["list_rcpt_grid_seqno_index_delete"]) - 1;
            try
            {
                if (Session["list_rcpt_grid_seqno_index_delete"] == null)
                {
                    GridView1.SelectedIndex = 0;
                }
                Session["seqno_list"] = GridView1.SelectedRow.Cells[1].Text;
            }
            catch
            {
                Session["seqno_list"] = null;
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
                string vPage = "recelist_rcpt.aspx";
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
                    Response.Write("<script>window.location.href = '../login.aspx';</script>");

                }


            }

        }

        protected void btnSearch_Click(object sender, EventArgs e)
        {
            UserClass UserLogin = (UserClass)Session["User"];

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSearch";
            ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = txt_seq.Text;
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_ino.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

            Session["prmAction_rcpt_gd"] = "GridSearch";
            Session["prmSeq_rcpt_gd"] = null;
            Session["prmTag_rcpt_gd"] = null;
            Session["prmDate_rcpt_gd"] = null;
            Session["prmPo_rcpt_gd"] = null;
            Session["prmIno_rcpt_gd"] = null;
            Session["prmJob_rcpt_gd"] = null;

            Session["list_rcpt_grid_seqno_index_delete"] = null;
            Session["list_rcpt_grid_seqno_gd"] = null;
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
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSelect";
            ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = txt_seq.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_ino.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

            Session["prmAction_rcpt_gd"] = "GridSelect";
            Session["prmSeq_rcpt_gd"] = null;
            Session["prmTag_rcpt_gd"] = null;
            Session["prmDate_rcpt_gd"] = null;
            Session["prmPo_rcpt_gd"] = null;
            Session["prmIno_rcpt_gd"] = null;
            Session["prmJob_rcpt_gd"] = null;

            Session["list_rcpt_grid_seqno_index_delete"] = null;
            Session["list_rcpt_grid_seqno_gd"] = null;

            Session["page_inder_gd"] = 0;

            GridView1.PageIndex = Convert.ToInt32(Session["page_inder_gd"]);
        }

        protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
        {
            Session["seqno_list"] = GridView1.SelectedRow.Cells[1].Text;

            Session["list_rcpt_grid_seqno_index_delete"] = GridView1.SelectedIndex + 1;
            foreach (GridViewRow gv in GridView1.Rows)
            {
                //Session["list_rcpt_grid_seqno"] = ((Label)GridView1.SelectedRow.FindControl("Label_seqno")).Text;
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
            Session["page_inder_gd"] = e.NewPageIndex;
        }

    }
}