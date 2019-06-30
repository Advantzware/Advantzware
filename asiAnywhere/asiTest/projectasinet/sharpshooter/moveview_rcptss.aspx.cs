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
    public partial class moveview_rcptss : System.Web.UI.Page
    {
        public moveview_rcptss()
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
            ObjectDataSource1.SelectParameters["PrmAction"].DefaultValue = "Select";

            ImageButton brwsorder = (ImageButton)Master.FindControl("moveview_rcpt");
            brwsorder.ImageUrl = "~/Images/view-receipts.jpg";

            if (Session["User"] != null)
            {

                string vUserId = UserLogin.UserName;
                string vPage = "movelist_rcpt.aspx";
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

        protected void FormView1_DataBound(object sender, EventArgs e)
        {
            // try
            //{
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];


            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                TextBox tag = (TextBox)FormView1.FindControl("vLocTextBox");
                tag.Focus();
            }
            // }
            // catch { }
        }

        protected void UpdateButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("vRnoLabel");
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            // TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = seq.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = date.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = transtime.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = jobno2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
            ObjectDataSource1.SelectParameters["prmStdCost"].DefaultValue = stdcost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCost_Uom"].DefaultValue = costuom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTQty"].DefaultValue = t_qty.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmFrtCost"].DefaultValue = frtcost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmExtCost"].DefaultValue = extcost.Text.Trim();

            ObjectDataSource1.SelectParameters["prmStackCode"].DefaultValue = stackcode.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmTotWt"].DefaultValue = tot_wt.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);



        }


        protected void FormView1_Unload(object sender, EventArgs e)
        {

            try
            {
                Label seq = (Label)FormView1.FindControl("vseqLabel");


                Session["move_list_seq"] = seq.Text.Trim();


            }
            catch { }
        }

        protected void PostButton_click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            Label seq = (Label)FormView1.FindControl("vseqLabel");
            Session["move_list_seq"] = seq.Text.Trim();

            string vError = null;
            itemhistory f1 = new itemhistory();

            f1.MoveRecepost(UserLogin.UserName, "PostMove", "", "", "", Convert.ToString(Session["move_list_seq"]), "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ref  vError);

            if (vError != "" && vError != null)
            {
                Response.Write("<script>alert('" + vError + "');</script>");
            }
            if (vError == null || vError == "")
            {
                FormView1.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href = 'movelist_rcptss.aspx';</script>");
            }
        }


    }
}